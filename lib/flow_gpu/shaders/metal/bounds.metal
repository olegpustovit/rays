// Generated from bounds.rays by Flow_gpu.Raster (lib/flow_gpu/test_raster.exe --write); do not edit.
#include <metal_stdlib>
using namespace metal;
kernel void chunk_bounds(
    device const float* buffer0 [[buffer(0)]],
    device float* buffer1 [[buffer(1)]],
    device const uint* buffer2 [[buffer(2)]],
    uint lid [[thread_position_in_threadgroup]]) {
  uint sb=0u;
  uint r4=1024u,r5=1023u,r7=0u,r9=1073741823u,r20=3u,r23=1u,r26=2u,r30=0,r31=0;
  float r32=0,r33=0,r34=0,r35=0,r36=0,r37=0;
  uint r65=6u,r77=4u,r80=5u,r84=0u;
  uint r6=(uint((long(lid)))&r5);
  uint r10=(uint((long(buffer2[r7*1+0])))&r9);
  uint r12=(r4==0u?0u:((r10+r5))/r4);
  sb|=(r4==0)?2u:0u;
  uint r13=(r6*r12);
  uint r15=(((long(r10)<long(r13)?1.0f:0.0f))!=0.0f?r10:r13);
  uint r16=(r15+r12);
  uint r18=(((long(r10)<long(r16)?1.0f:0.0f))!=0.0f?r10:r16);
  float r19=(long(r15)<long(r18)?1.0f:0.0f);
  if(r19!=0.0f) {
    uint r21=(r15*r20);
    float r22=buffer0[r21*1+0];
    float r25=buffer0[((r21+r23))*1+0];
    float r28=buffer0[((r21+r26))*1+0];
    r31=r15;
    r32=r22;
    r33=r25;
    r34=r28;
    r35=r22;
    r36=r25;
    r37=r28;
    for(r30=(0u);r30<r12;r30++){
  uint r38=3u,r41=1u,r44=2u,r47=0u;
  float r48=0.0f,r49=0.0f,r50=0.0f,r51=0.0f,r52=0.0f,r53=0.0f,r56=0.0f;
  if(r19!=0.0f) {
    uint r39=(r31*r38);
    float r40=buffer0[r39*1+0];
    float r43=buffer0[((r39+r41))*1+0];
    float r46=buffer0[((r39+r44))*1+0];
    r47=(r31+r41);
    r48=min(r32,r40);
    r49=min(r33,r43);
    r50=min(r34,r46);
    r51=max(r35,r40);
    r52=max(r36,r43);
    r53=max(r37,r46);
    r56=(((((long(r47)>=long(r18)?1.0f:0.0f))==0.0f))==0.0f);
  }
  bool stop30=r56!=0.0f;
  uint next30_0=r47;
  float next30_1=r48;
  float next30_2=r49;
  float next30_3=r50;
  float next30_4=r51;
  float next30_5=r52;
  float next30_6=r53;
  r31=next30_0;
  r32=next30_1;
  r33=next30_2;
  r34=next30_3;
  r35=next30_4;
  r36=next30_5;
  r37=next30_6;
  if(stop30)break;
    }
    uint r66=(r6*r65);
    uint r67=(buffer1[r66]=(r32),1u);
    uint r69=(buffer1[((r66+r23))]=(r33),1u);
    uint r72=(buffer1[((r66+r26))]=(r34),1u);
    uint r74=(buffer1[((r66+r20))]=(r35),1u);
    uint r75=(r72+r74);
    uint r79=(buffer1[((r66+r77))]=(r36),1u);
    uint r82=(buffer1[((r66+r80))]=(r37),1u);
    uint r83=(r79+r82);
    r84=(((((r67+r69))+r75))+r83);
  }
  uint r85=(r19!=0.0f?r84:r7);
  (void)float(r85);
}
