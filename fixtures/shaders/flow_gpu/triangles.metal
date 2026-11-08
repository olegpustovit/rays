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
  float r41=0.0f,r43=0.0f,r45=0.0f;
  uint r46=1u;
  float r48=0.0f,r49=0.0f,r51=0.0f,r52=0.0f,r55=0.0f,r60=0.0f;
  uint r61=2u,r66=3u;
  float r71=0.0f,r72=0.0f,r73=0.0f,r74=0.0f;
  uint r76=1u,r79=2u;
  float r83=0.0f;
  uint r84=3u;
  long r86=0;
  float r94=0.0f,r96=0.0f;
  uint r98=1u,r101=2u,r106=3u,r107=0u,r108=3u,r111=1u,r114=2u;
  float r213=0.0f;
  uint r217=0u,r218=0u;
  float r220=0.0f,r222=0.0f,r224=0.0f,r226=0.0f,r228=0.0f,r229=0.0f,r231=0.0f,r232=0.0f,r235=0.0f,r236=0.0f,r239=0.0f,r240=0.0f;
  float r243=0.0f,r244=0.0f,r263=0.0f,r273=0.0f,r284=0.0f,r285=0.0f,r316=0.0f;
  long r324=0,r326=0,r328=0;
  uint r329=0u;
  float r331=0.0f,r342=0.0f,r343=0.0f,r374=0.0f;
  long r382=0,r384=0,r386=0,r388=0,r389=long(-1L);
  float r391=0.0f,r402=0.0f,r403=0.0f,r434=0.0f;
  long r442=0,r444=0,r446=0,r448=0;
  float r450=0.0f,r461=0.0f,r462=0.0f,r493=0.0f;
  long r501=0,r503=0,r505=0,r507=0;
  uint r509=1u;
  float r510=0.0f;
  uint r511=1u;
  float r512=0.0f,r514=0.0f;
  long r519=0,r521=0,r522=0;
  float r524=0.0f,r526=0.0f;
  uint r528=1u,r531=2u,r535=0u;
  float r537=0.0f;
  uint r539=1u,r542=2u,r546=0u;
  float r548=0.0f;
  uint r550=1u,r553=2u,r557=0u,r559=1u,r562=2u,r566=0u,r567=0u,r568=0u,r570=3u,r578=0u,r585=0u,r592=0u;
  uint r599=0u,r600=0u,r601=0u,r604=6u,r605=0u,r606=0u,r607=0u,r608=0u,r609=0u,r610=0u,r612=0u,r613=0u;
  int r614=0;
  uint r615=0,r625=0u,scan626_before=0u,scan626_total=0u;
  float r631=0.0f;
  uint r632=0u,r634=0u,r635=0u;
  long r0=long(gid);
  long r11=long(buffer7[(2u)*1+0]);
  long r13=long(buffer7[(3u)*1+0]);
  long r14=(long(r9)==0?0:long(r9)==-1?as_type<long>(0ul-ulong(r0)):r0/long(r9));
  sb|=(r9==0)?2u:0u;
  long r15=(long(r9)==0||long(r9)==-1?0:r0%long(r9));
  sb|=(r9==0)?2u:0u;
  long r18=(long(r9)==0?0:long(r9)==-1?as_type<long>(0ul-ulong((as_type<long>(ulong(r11)+ulong(long(r16)))))):(as_type<long>(ulong(r11)+ulong(long(r16))))/long(r9));
  sb|=(r9==0)?2u:0u;
  long r20=(long(r9)==0?0:long(r9)==-1?as_type<long>(0ul-ulong((as_type<long>(ulong(r18)+ulong(long(r16)))))):(as_type<long>(ulong(r18)+ulong(long(r16))))/long(r9));
  sb|=(r9==0)?2u:0u;
  float r22=(r13==long(r21)?1.0f:0.0f);
  if(r22!=0.0f) {
    r23=(r0<r11?1.0f:0.0f);
    if(r23!=0.0f) {
      long r25=long(buffer7[r24*1+0]);
      long r27=long(buffer7[r26*1+0]);
      long r29=long(buffer7[r28*1+0]);
      long r30=long(buffer2[r0*1+0]);
      long r32=long(buffer2[((uint(r0)+r26))*1+0]);
      long r33=as_type<long>(ulong(r32)-ulong(r30));
      long r34=as_type<long>(ulong(r0)*ulong(long(r28)));
      long r37=as_type<long>(ulong((as_type<long>(ulong(r30)-ulong(r34))))*ulong(long(r36)));
      long r38=as_type<long>(ulong(r29)*ulong(long(r28)));
      r41=(r0==long(r24)?1.0f:0.0f);
      if(r41!=0.0f) {
        r43=(r30==long((0u))?1.0f:0.0f);
      }
      r45=(r41!=0.0f?r43:(as_type<float>(0x3f800000u)));
      if(r45!=0.0f) {
        r48=((as_type<long>(ulong(r0)+ulong(long(r46))))==r29?1.0f:0.0f);
        if(r48!=0.0f) {
          r49=(r32==r27?1.0f:0.0f);
        }
        r51=(r48!=0.0f?r49:(as_type<float>(0x3f800000u)));
      }
      r52=(r45!=0.0f&&r51!=0.0f);
      if(r52!=0.0f) {
        r55=((long(buffer3[r0*1+0]))==long((0u))?1.0f:0.0f);
        if(r55!=0.0f) {
          r60=(((r33==long((3u))?1.0f:0.0f))!=0.0f||((r33==long((4u))?1.0f:0.0f))!=0.0f);
          if(r60!=0.0f) {
            long r67=as_type<long>(ulong((as_type<long>(ulong(r33)-ulong(long(r61)))))*ulong(long(r66)));
            r71=(((r30>=(as_type<long>(ulong(r0)*ulong(long(r61))))?1.0f:0.0f))!=0.0f&&((((r32<=r27?1.0f:0.0f))!=0.0f&&(((as_type<long>(ulong(r37)+ulong(r67)))<=(as_type<long>(ulong((as_type<long>(ulong(r27)-ulong(r38))))*ulong(long(r36))))?1.0f:0.0f))!=0.0f))!=0.0f);
          }
          r72=(r60!=0.0f&&r71!=0.0f);
        }
        r73=(r55!=0.0f&&r72!=0.0f);
      }
      r74=(r52!=0.0f&&r73!=0.0f);
      if(r74!=0.0f) {
        long r75=long(buffer1[r30*1+0]);
        long r78=long(buffer1[((uint(r30)+r76))*1+0]);
        long r81=long(buffer1[((uint(r30)+r79))*1+0]);
        r83=(r33==long((4u))?1.0f:0.0f);
        if(r83!=0.0f) {
          r86=long(buffer1[((uint(r30)+r84))*1+0]);
        }
        long r87=(r83!=0.0f?r86:r81);
        r94=(((r75<r25?1.0f:0.0f))!=0.0f&&((((r78<r25?1.0f:0.0f))!=0.0f&&((((r81<r25?1.0f:0.0f))!=0.0f&&((r87<r25?1.0f:0.0f))!=0.0f))!=0.0f))!=0.0f);
        if(r94!=0.0f) {
          r96=(r33==long((3u))?1.0f:0.0f);
          if(r96!=0.0f) {
            uint r97=(buffer4[uint(r37)]=uint(r75),1u);
            uint r100=(buffer4[((uint(r37)+r98))]=uint(r78),1u);
            uint r103=(buffer4[((uint(r37)+r101))]=uint(r81),1u);
            uint r104=(r100+r103);
            r107=(((r97+r104))-r106);
          }
          if(r96==0.0f) {
            uint r109=(uint(r75)*r108);
            float r110=buffer0[r109*1+0];
            float r113=buffer0[((r109+r111))*1+0];
            float r116=buffer0[((r109+r114))*1+0];
            uint r117=(uint(r78)*r108);
            float r118=buffer0[r117*1+0];
            float r120=buffer0[((r117+r111))*1+0];
            float r122=buffer0[((r117+r114))*1+0];
            uint r123=(uint(r81)*r108);
            float r124=buffer0[r123*1+0];
            float r126=buffer0[((r123+r111))*1+0];
            float r128=buffer0[((r123+r114))*1+0];
            uint r129=(uint(r87)*r108);
            float r130=buffer0[r129*1+0];
            float r132=buffer0[((r129+r111))*1+0];
            float r134=buffer0[((r129+r114))*1+0];
            float r157=max((max((max((fabs(r110)),(max((fabs(r113)),(fabs(r116)))))),(max((fabs(r118)),(max((fabs(r120)),(fabs(r122)))))))),(max((max((fabs(r124)),(max((fabs(r126)),(fabs(r128)))))),(max((fabs(r130)),(max((fabs(r132)),(fabs(r134)))))))));
            float r162=(((((r157*r157))*(as_type<float>(0x358637bdu))))+(as_type<float>(0x2edbe6ffu)));
            float r208=fabs(((((((((((r113-r120))*((r116+r122))))+((((r120-r126))*((r122+r128))))))+((((r126-r132))*((r128+r134))))))+((((r132-r113))*((r134+r116)))))));
            float r209=fabs(((((((((((r116-r122))*((r110+r118))))+((((r122-r128))*((r118+r124))))))+((((r128-r134))*((r124+r130))))))+((((r134-r116))*((r130+r110)))))));
            float r210=fabs(((((((((((r110-r118))*((r113+r120))))+((((r118-r124))*((r120+r126))))))+((((r124-r130))*((r126+r132))))))+((((r130-r110))*((r132+r113)))))));
            r213=(((r208>=r209))!=0.0f&&((r208>=r210))!=0.0f);
            if(r213==0.0f) {
              r217=(((r209>=r210))!=0.0f?(1u):(2u));
            }
            uint r219=(r213!=0.0f?r218:r217);
            r220=(long(r219)==long(r218)?1.0f:0.0f);
            if(r220!=0.0f) {
              r222=(r208-(max(r209,r210)));
            }
            if(r220==0.0f) {
              r224=(long(r219)==long((1u))?1.0f:0.0f);
              if(r224!=0.0f) {
                r226=(r209-(max(r208,r210)));
              }
              if(r224==0.0f) {
                r228=(r210-(max(r208,r209)));
              }
              r229=(r224!=0.0f?r226:r228);
              r231=r110;
              r232=(r224!=0.0f?r116:r113);
            }
            float r233=(r220!=0.0f?r113:r231);
            float r234=(r220!=0.0f?r116:r232);
            if(r220==0.0f) {
              r235=r118;
              r236=(r224!=0.0f?r122:r120);
            }
            float r237=(r220!=0.0f?r120:r235);
            float r238=(r220!=0.0f?r122:r236);
            if(r220==0.0f) {
              r239=r124;
              r240=(r224!=0.0f?r128:r126);
            }
            float r241=(r220!=0.0f?r126:r239);
            float r242=(r220!=0.0f?r128:r240);
            if(r220==0.0f) {
              r243=r130;
              r244=(r224!=0.0f?r134:r132);
            }
            float r245=(r220!=0.0f?r132:r243);
            float r246=(r220!=0.0f?r134:r244);
            float r261=(((((((((r233*r238))-((r237*r234))))+((((r237*r242))-((r241*r238))))))+((((r241*r246))-((r245*r242))))))+((((r245*r234))-((r233*r246)))));
            r263=(r261>(as_type<float>(0x0u)));
            float r266=(r263!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0xbf800000u)));
            r273=(((r157<(as_type<float>(0x5368d4a5u))))!=0.0f&&((((((r220!=0.0f?r222:r229))>r162))!=0.0f&&(((fabs(r261))>r162))!=0.0f))!=0.0f);
            if(r273!=0.0f) {
              float r281=(((((((r233-r245))*((r238-r246))))-((((r234-r246))*((r237-r245))))))*r266);
              r284=(r281<((r162*(as_type<float>(0xbf800000u)))));
              if(r284==0.0f) {
                r285=(r281>r162);
                if(r285!=0.0f) {
                  float r293=(((((((r233-r245))*((r242-r246))))-((((r234-r246))*((r241-r245))))))*r266);
                  float r301=(((((((r237-r233))*((r242-r234))))-((((r238-r234))*((r241-r233))))))*r266);
                  float r309=(((((((r245-r237))*((r242-r238))))-((((r246-r238))*((r241-r237))))))*r266);
                  float r311=(r162*(as_type<float>(0xbf800000u)));
                  r316=(((r293<r311))!=0.0f||((((r301<r311))!=0.0f||((r309<r311))!=0.0f))!=0.0f);
                  if(r316==0.0f) {
                    r324=(((((r293>r162))!=0.0f&&((((r301>r162))!=0.0f&&((r309>r162))!=0.0f))!=0.0f))!=0.0f?long((0u)):(long(-1L)));
                  }
                  r326=(r316!=0.0f?long((1u)):r324);
                }
                r328=(r285!=0.0f?r326:(long(-1L)));
              }
              long r330=(r284!=0.0f?long(r329):r328);
              r331=(r330==long(r329)?1.0f:0.0f);
              if(r331!=0.0f) {
                float r339=(((((((r237-r233))*((r242-r234))))-((((r238-r234))*((r241-r233))))))*r266);
                r342=(r339<((r162*(as_type<float>(0xbf800000u)))));
                if(r342==0.0f) {
                  r343=(r339>r162);
                  if(r343!=0.0f) {
                    float r351=(((((((r237-r233))*((r246-r234))))-((((r238-r234))*((r245-r233))))))*r266);
                    float r359=(((((((r241-r237))*((r246-r238))))-((((r242-r238))*((r245-r237))))))*r266);
                    float r367=(((((((r233-r241))*((r246-r242))))-((((r234-r242))*((r245-r241))))))*r266);
                    float r369=(r162*(as_type<float>(0xbf800000u)));
                    r374=(((r351<r369))!=0.0f||((((r359<r369))!=0.0f||((r367<r369))!=0.0f))!=0.0f);
                    if(r374==0.0f) {
                      r382=(((((r351>r162))!=0.0f&&((((r359>r162))!=0.0f&&((r367>r162))!=0.0f))!=0.0f))!=0.0f?long((0u)):(long(-1L)));
                    }
                    r384=(r374!=0.0f?long((1u)):r382);
                  }
                  r386=(r343!=0.0f?r384:(long(-1L)));
                }
                r388=(r342!=0.0f?long((0u)):r386);
              }
              long r390=(r331!=0.0f?r388:r389);
              r391=(r390==long(r329)?1.0f:0.0f);
              if(r391!=0.0f) {
                float r399=(((((((r241-r237))*((r246-r238))))-((((r242-r238))*((r245-r237))))))*r266);
                r402=(r399<((r162*(as_type<float>(0xbf800000u)))));
                if(r402==0.0f) {
                  r403=(r399>r162);
                  if(r403!=0.0f) {
                    float r411=(((((((r241-r237))*((r234-r238))))-((((r242-r238))*((r233-r237))))))*r266);
                    float r419=(((((((r245-r241))*((r234-r242))))-((((r246-r242))*((r233-r241))))))*r266);
                    float r427=(((((((r237-r245))*((r234-r246))))-((((r238-r246))*((r233-r245))))))*r266);
                    float r429=(r162*(as_type<float>(0xbf800000u)));
                    r434=(((r411<r429))!=0.0f||((((r419<r429))!=0.0f||((r427<r429))!=0.0f))!=0.0f);
                    if(r434==0.0f) {
                      r442=(((((r411>r162))!=0.0f&&((((r419>r162))!=0.0f&&((r427>r162))!=0.0f))!=0.0f))!=0.0f?long((0u)):(long(-1L)));
                    }
                    r444=(r434!=0.0f?long((1u)):r442);
                  }
                  r446=(r403!=0.0f?r444:(long(-1L)));
                }
                r448=(r402!=0.0f?long((0u)):r446);
              }
              long r449=(r391!=0.0f?r448:r389);
              r450=(r449==long(r329)?1.0f:0.0f);
              if(r450!=0.0f) {
                float r458=(((((((r245-r241))*((r234-r242))))-((((r246-r242))*((r233-r241))))))*r266);
                r461=(r458<((r162*(as_type<float>(0xbf800000u)))));
                if(r461==0.0f) {
                  r462=(r458>r162);
                  if(r462!=0.0f) {
                    float r470=(((((((r245-r241))*((r238-r242))))-((((r246-r242))*((r237-r241))))))*r266);
                    float r478=(((((((r233-r245))*((r238-r246))))-((((r234-r246))*((r237-r245))))))*r266);
                    float r486=(((((((r241-r233))*((r238-r234))))-((((r242-r234))*((r237-r233))))))*r266);
                    float r488=(r162*(as_type<float>(0xbf800000u)));
                    r493=(((r470<r488))!=0.0f||((((r478<r488))!=0.0f||((r486<r488))!=0.0f))!=0.0f);
                    if(r493==0.0f) {
                      r501=(((((r470>r162))!=0.0f&&((((r478>r162))!=0.0f&&((r486>r162))!=0.0f))!=0.0f))!=0.0f?long((0u)):(long(-1L)));
                    }
                    r503=(r493!=0.0f?long((1u)):r501);
                  }
                  r505=(r462!=0.0f?r503:(long(-1L)));
                }
                r507=(r461!=0.0f?long((0u)):r505);
              }
              r510=(r330==long(r509)?1.0f:0.0f);
              if(r510==0.0f) {
                r512=(r390==long(r511)?1.0f:0.0f);
                if(r512==0.0f) {
                  r514=(r449==long((1u))?1.0f:0.0f);
                  if(r514==0.0f) {
                    r519=(((((r450!=0.0f?r507:r389))==long((1u))?1.0f:0.0f))!=0.0f?long((3u)):(long(-1L)));
                  }
                  r521=(r514!=0.0f?long((2u)):r519);
                }
                r522=(r512!=0.0f?long(r511):r521);
              }
              long r523=(r510!=0.0f?long(r329):r522);
              r524=(r523<long(r329)?1.0f:0.0f);
              if(r524==0.0f) {
                r526=(r523==long((0u))?1.0f:0.0f);
                if(r526!=0.0f) {
                  uint r527=(buffer4[uint(r37)]=uint(r87),1u);
                  uint r530=(buffer4[((uint(r37)+r528))]=uint(r75),1u);
                  uint r533=(buffer4[((uint(r37)+r531))]=uint(r78),1u);
                  uint r534=(r530+r533);
                  r535=(r527+r534);
                }
                if(r526==0.0f) {
                  r537=(r523==long((1u))?1.0f:0.0f);
                  if(r537!=0.0f) {
                    uint r538=(buffer4[uint(r37)]=uint(r75),1u);
                    uint r541=(buffer4[((uint(r37)+r539))]=uint(r78),1u);
                    uint r544=(buffer4[((uint(r37)+r542))]=uint(r81),1u);
                    uint r545=(r541+r544);
                    r546=(r538+r545);
                  }
                  if(r537==0.0f) {
                    r548=(r523==long((2u))?1.0f:0.0f);
                    if(r548!=0.0f) {
                      uint r549=(buffer4[uint(r37)]=uint(r78),1u);
                      uint r552=(buffer4[((uint(r37)+r550))]=uint(r81),1u);
                      uint r555=(buffer4[((uint(r37)+r553))]=uint(r87),1u);
                      uint r556=(r552+r555);
                      r557=(r549+r556);
                    }
                    if(r548==0.0f) {
                      uint r558=(buffer4[uint(r37)]=uint(r81),1u);
                      uint r561=(buffer4[((uint(r37)+r559))]=uint(r87),1u);
                      uint r564=(buffer4[((uint(r37)+r562))]=uint(r75),1u);
                      uint r565=(r561+r564);
                      r566=(r558+r565);
                    }
                    r567=(r548!=0.0f?r557:r566);
                  }
                  r568=(r537!=0.0f?r546:r567);
                }
                uint r571=(uint(r37)+r570);
                if(r526!=0.0f) {
                  uint r572=(buffer4[r571]=uint(r78),1u);
                  uint r574=(buffer4[((r571+r528))]=uint(r81),1u);
                  uint r576=(buffer4[((r571+r531))]=uint(r87),1u);
                  uint r577=(r574+r576);
                  r578=(r572+r577);
                }
                if(r526==0.0f) {
                  if(r537!=0.0f) {
                    uint r579=(buffer4[r571]=uint(r75),1u);
                    uint r581=(buffer4[((r571+r539))]=uint(r81),1u);
                    uint r583=(buffer4[((r571+r542))]=uint(r87),1u);
                    uint r584=(r581+r583);
                    r585=(r579+r584);
                  }
                  if(r537==0.0f) {
                    if(r548!=0.0f) {
                      uint r586=(buffer4[r571]=uint(r75),1u);
                      uint r588=(buffer4[((r571+r550))]=uint(r78),1u);
                      uint r590=(buffer4[((r571+r553))]=uint(r87),1u);
                      uint r591=(r588+r590);
                      r592=(r586+r591);
                    }
                    if(r548==0.0f) {
                      uint r593=(buffer4[r571]=uint(r75),1u);
                      uint r595=(buffer4[((r571+r559))]=uint(r78),1u);
                      uint r597=(buffer4[((r571+r562))]=uint(r81),1u);
                      uint r598=(r595+r597);
                      r599=(r593+r598);
                    }
                    r600=(r548!=0.0f?r592:r599);
                  }
                  r601=(r537!=0.0f?r585:r600);
                }
                uint r602=(r526!=0.0f?r578:r601);
                r605=(((((r526!=0.0f?r535:r568))+r602))-r604);
              }
              r606=(r524!=0.0f?r509:r605);
            }
            r607=(r273!=0.0f?r606:r111);
          }
          r608=(r96!=0.0f?r107:r607);
        }
        r609=(r94!=0.0f?r608:r76);
      }
      r610=(r74!=0.0f?r609:r26);
    }
    r612=(r23!=0.0f?r610:(0u));
  }
  if(r22==0.0f) {
    bool over614=r20>long(r613) && ulong(r20-long(r613))>4096uL;
    sb|=over614?1u:0u;
    r615=r613;
    for(r614=long(r613);r614<(over614?long(r613):r20);r614++){
  uint r616=256u;
  float r619=0.0f;
  long r620=0;
  uint r623=0u;
  if(r22==0.0f) {
    long r617=as_type<long>(ulong(r614)*ulong(long(r616)));
    long r618=as_type<long>(ulong(r15)+ulong(r617));
    r619=(r618<r18?1.0f:0.0f);
    if(r619!=0.0f) {
      r620=long(buffer5[r618*1+0]);
    }
    uint r622=(r619!=0.0f?uint(r620):(0u));
    r623=(r615+r622);
  }
  uint next614_0=r623;
  r615=next614_0;
    }
    r625=r615;
  }
  threadgroup uint scan626[256];
  {uint v=((r22!=0.0f?r612:r625));scan626[lid]=v;for(uint d=1u;d<256u;d<<=1u){threadgroup_barrier(mem_flags::mem_threadgroup);uint w=lid>=d?scan626[lid-d]:0u;threadgroup_barrier(mem_flags::mem_threadgroup);v+=w;scan626[lid]=v;}threadgroup_barrier(mem_flags::mem_threadgroup);scan626_before=v-((r22!=0.0f?r612:r625));scan626_total=scan626[256-1];}
  uint r628=scan626_total;
  float r629=(r15==long(r21)?1.0f:0.0f);
  if(r629!=0.0f) {
    r631=(r13==long((0u))?1.0f:0.0f);
    if(r631!=0.0f) {
      r632=(buffer5[uint(r14)]=r628,1u);
    }
    if(r631==0.0f) {
      r634=(buffer6[(0u)]=r628,1u);
    }
    r635=(r631!=0.0f?r632:r634);
  }
  uint r636=(r629!=0.0f?r635:r21);
  (void)float(r636);
}
