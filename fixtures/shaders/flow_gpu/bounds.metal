#include <metal_stdlib>
using namespace metal;
kernel void chunk_bounds(
    device const float* buffer0 [[buffer(0)]],
    device float* buffer1 [[buffer(1)]],
    device const uint* buffer2 [[buffer(2)]],
    uint gid [[thread_position_in_grid]],
    uint lid [[thread_position_in_threadgroup]]) {
  uint sb=0u;
  uint r5=1u,r7=0u,r9=1073741823u,r17=3u,r20=1u,r23=2u;
  long r27=0,r28=0;
  float r29=0,r30=0,r31=0,r32=0,r33=0,r34=0;
  uint r62=6u,r74=4u,r77=5u,r81=0u;
  long r0=long(gid);
  long r6=long(buffer2[r5*1+0]);
  uint r10=(uint((long(buffer2[r7*1+0])))&r9);
  long r11=as_type<long>(ulong(r6)-ulong(long(r5)));
  long r13=(r6==0?0:r6==-1?as_type<long>(0ul-ulong((as_type<long>(ulong(long(r10))+ulong(r11))))):(as_type<long>(ulong(long(r10))+ulong(r11)))/r6);
  sb|=(r6==0)?2u:0u;
  float r16=(((r0<long(r10)?1.0f:0.0f))!=0.0f&&((r0<r6?1.0f:0.0f))!=0.0f);
  if(r16!=0.0f) {
    uint r18=(uint(r0)*r17);
    float r19=buffer0[r18*1+0];
    float r22=buffer0[((r18+r20))*1+0];
    float r25=buffer0[((r18+r23))*1+0];
    r28=r0;
    r29=r19;
    r30=r22;
    r31=r25;
    r32=r19;
    r33=r22;
    r34=r25;
    for(r27=long((0u));r27<r13;r27++){
  uint r35=3u,r38=1u,r41=2u;
  long r44=0;
  float r45=0.0f,r46=0.0f,r47=0.0f,r48=0.0f,r49=0.0f,r50=0.0f,r53=0.0f;
  if(r16!=0.0f) {
    uint r36=(uint(r28)*r35);
    float r37=buffer0[r36*1+0];
    float r40=buffer0[((r36+r38))*1+0];
    float r43=buffer0[((r36+r41))*1+0];
    r44=as_type<long>(ulong(r28)+ulong(r6));
    r45=min(r29,r37);
    r46=min(r30,r40);
    r47=min(r31,r43);
    r48=max(r32,r37);
    r49=max(r33,r40);
    r50=max(r34,r43);
    r53=(((((r44>=long(r10)?1.0f:0.0f))==0.0f))==0.0f);
  }
  bool stop27=r53!=0.0f;
  long next27_0=r44;
  float next27_1=r45;
  float next27_2=r46;
  float next27_3=r47;
  float next27_4=r48;
  float next27_5=r49;
  float next27_6=r50;
  r28=next27_0;
  r29=next27_1;
  r30=next27_2;
  r31=next27_3;
  r32=next27_4;
  r33=next27_5;
  r34=next27_6;
  if(stop27)break;
    }
    uint r63=(uint(r0)*r62);
    uint r64=(buffer1[r63]=(r29),1u);
    uint r66=(buffer1[((r63+r20))]=(r30),1u);
    uint r69=(buffer1[((r63+r23))]=(r31),1u);
    uint r71=(buffer1[((r63+r17))]=(r32),1u);
    uint r72=(r69+r71);
    uint r76=(buffer1[((r63+r74))]=(r33),1u);
    uint r79=(buffer1[((r63+r77))]=(r34),1u);
    uint r80=(r76+r79);
    r81=(((((r64+r66))+r72))+r80);
  }
  uint r82=(r16!=0.0f?r81:r7);
  (void)float(r82);
}
