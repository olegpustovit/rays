// Generated from bounds.rays by Flow_gpu.Raster (lib/flow_gpu/test_raster.exe --write); do not edit.
#include <metal_stdlib>
using namespace metal;
kernel void chunk_bounds(
    device const float* buffer0 [[buffer(0)]],
    device float* buffer1 [[buffer(1)]],
    device const uint* buffer2 [[buffer(2)]],
    uint lid [[thread_position_in_threadgroup]]) {
  uint sb=0u;
  uint r4=1024u,r5=1023u,r7=0u,r9=1073741823u,r14=3u,r17=1u,r20=2u,r24=0,r25=0;
  float r26=0,r27=0,r28=0,r29=0,r30=0,r31=0;
  uint r60=6u,r72=4u,r75=5u,r79=0u;
  uint r6=(uint((long(lid)))&r5);
  uint r10=(uint((long(buffer2[r7*1+0])))&r9);
  uint r12=(r4==0u?0u:((r10+r5))/r4);
  sb|=(r4==0)?2u:0u;
  float r13=(long(r6)<long(r10)?1.0f:0.0f);
  if(r13!=0.0f) {
    uint r15=(r6*r14);
    float r16=buffer0[r15*1+0];
    float r19=buffer0[((r15+r17))*1+0];
    float r22=buffer0[((r15+r20))*1+0];
    r25=r6;
    r26=r16;
    r27=r19;
    r28=r22;
    r29=r16;
    r30=r19;
    r31=r22;
    for(r24=(0u);r24<r12;r24++){
  uint r32=3u,r35=1u,r38=2u,r41=1024u,r42=0u;
  float r43=0.0f,r44=0.0f,r45=0.0f,r46=0.0f,r47=0.0f,r48=0.0f,r51=0.0f;
  if(r13!=0.0f) {
    uint r33=(r25*r32);
    float r34=buffer0[r33*1+0];
    float r37=buffer0[((r33+r35))*1+0];
    float r40=buffer0[((r33+r38))*1+0];
    r42=(r25+r41);
    r43=min(r26,r34);
    r44=min(r27,r37);
    r45=min(r28,r40);
    r46=max(r29,r34);
    r47=max(r30,r37);
    r48=max(r31,r40);
    r51=(((((long(r42)>=long(r10)?1.0f:0.0f))==0.0f))==0.0f);
  }
  bool stop24=r51!=0.0f;
  uint next24_0=r42;
  float next24_1=r43;
  float next24_2=r44;
  float next24_3=r45;
  float next24_4=r46;
  float next24_5=r47;
  float next24_6=r48;
  r25=next24_0;
  r26=next24_1;
  r27=next24_2;
  r28=next24_3;
  r29=next24_4;
  r30=next24_5;
  r31=next24_6;
  if(stop24)break;
    }
    uint r61=(r6*r60);
    uint r62=(buffer1[r61]=(r26),1u);
    uint r64=(buffer1[((r61+r17))]=(r27),1u);
    uint r67=(buffer1[((r61+r20))]=(r28),1u);
    uint r69=(buffer1[((r61+r14))]=(r29),1u);
    uint r70=(r67+r69);
    uint r74=(buffer1[((r61+r72))]=(r30),1u);
    uint r77=(buffer1[((r61+r75))]=(r31),1u);
    uint r78=(r74+r77);
    r79=(((((r62+r64))+r70))+r78);
  }
  uint r80=(r13!=0.0f?r79:r7);
  (void)float(r80);
}
