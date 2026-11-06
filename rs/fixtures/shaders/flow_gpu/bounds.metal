#include <metal_stdlib>
using namespace metal;
kernel void chunk_bounds(
    device const float* buffer0 [[buffer(0)]],
    device float* buffer1 [[buffer(1)]],
    device const uint* buffer2 [[buffer(2)]],
    uint gid [[thread_position_in_grid]],
    uint lid [[thread_position_in_threadgroup]]) {
  uint sb=0u;
  uint r5=1u,r7=0u,r9=1073741823u,r15=3u,r18=1u,r21=2u;
  long r25=0,r26=0;
  float r27=0,r28=0,r29=0,r30=0,r31=0,r32=0;
  uint r60=6u,r72=4u,r75=5u,r79=0u;
  long r0=long(gid);
  long r6=long(buffer2[r5*1+0]);
  uint r10=(uint((long(buffer2[r7*1+0])))&r9);
  long r11=(as_type<long>(ulong((ulong(r6)-ulong(long(r5))))<<1)>>1);
  long r13=(as_type<long>(ulong((r6==0?0:((as_type<long>(ulong((ulong(long(r10))+ulong(r11)))<<1)>>1))/r6))<<1)>>1);
  sb|=(r6==0)?2u:0u;
  float r14=(r0<long(r10)?1.0f:0.0f);
  if(r14!=0.0f) {
    uint r16=(uint(r0)*r15);
    float r17=buffer0[r16*1+0];
    float r20=buffer0[((r16+r18))*1+0];
    float r23=buffer0[((r16+r21))*1+0];
    r26=r0;
    r27=r17;
    r28=r20;
    r29=r23;
    r30=r17;
    r31=r20;
    r32=r23;
    for(r25=long((0u));r25<r13;r25++){
  uint r33=3u,r36=1u,r39=2u;
  long r42=0;
  float r43=0.0f,r44=0.0f,r45=0.0f,r46=0.0f,r47=0.0f,r48=0.0f,r51=0.0f;
  if(r14!=0.0f) {
    uint r34=(uint(r26)*r33);
    float r35=buffer0[r34*1+0];
    float r38=buffer0[((r34+r36))*1+0];
    float r41=buffer0[((r34+r39))*1+0];
    r42=(as_type<long>(ulong((ulong(r26)+ulong(r6)))<<1)>>1);
    r43=min(r27,r35);
    r44=min(r28,r38);
    r45=min(r29,r41);
    r46=max(r30,r35);
    r47=max(r31,r38);
    r48=max(r32,r41);
    r51=(((((r42>=long(r10)?1.0f:0.0f))==0.0f))==0.0f);
  }
  bool stop25=r51!=0.0f;
  long next25_0=r42;
  float next25_1=r43;
  float next25_2=r44;
  float next25_3=r45;
  float next25_4=r46;
  float next25_5=r47;
  float next25_6=r48;
  r26=next25_0;
  r27=next25_1;
  r28=next25_2;
  r29=next25_3;
  r30=next25_4;
  r31=next25_5;
  r32=next25_6;
  if(stop25)break;
    }
    uint r61=(uint(r0)*r60);
    uint r62=(buffer1[r61]=(r27),1u);
    uint r64=(buffer1[((r61+r18))]=(r28),1u);
    uint r67=(buffer1[((r61+r21))]=(r29),1u);
    uint r69=(buffer1[((r61+r15))]=(r30),1u);
    uint r70=(r67+r69);
    uint r74=(buffer1[((r61+r72))]=(r31),1u);
    uint r77=(buffer1[((r61+r75))]=(r32),1u);
    uint r78=(r74+r77);
    r79=(((((r62+r64))+r70))+r78);
  }
  uint r80=(r14!=0.0f?r79:r7);
  (void)float(r80);
}
